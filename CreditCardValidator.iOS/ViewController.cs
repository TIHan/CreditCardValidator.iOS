using System;

using UIKit;
using CreditCardValidation.Common;
using System.Collections.Generic;

namespace CreditCardValidator.iOS
{
	public partial class ViewController : UIViewController
	{
		static readonly ICreditCardValidator _validator = new SimpleCreditCardValidator();

		public ViewController(IntPtr handle) : base(handle)
		{
		}


		public override void ViewWillAppear(bool animated)
		{
			base.ViewWillAppear(animated);
			ErrorMessagesTextField.Text = string.Empty;
			CreditCardTextField.Text = string.Empty;

		}

		public override void UpdateViewConstraints()
		{
			base.UpdateViewConstraints();
			SetupConstraints();
		}

		partial void ValidateButton_TouchUpInside(UIButton sender)
		{
			ErrorMessagesTextField.Text = String.Empty;
			string errorMessage;
			bool isValid = _validator.IsCCValid(CreditCardTextField.Text, out errorMessage);


			if (isValid)
			{
				UIViewController ctlr = this.Storyboard.InstantiateViewController("ValidCreditCardController");
				NavigationController.PushViewController(ctlr, true);
			}
			else
			{
				InvokeOnMainThread(() =>
				{
					ErrorMessagesTextField.Text = errorMessage;
				});
			}
		}


		void SetupConstraints()
		{
			List<NSLayoutConstraint> constraints = new List<NSLayoutConstraint>();

			BuildConstraintsForCreditCardTextField(constraints);
			BuildConstraintsForValidateButton(constraints);
			BuildConstraintsForErrorMessage(constraints);

			View.AddConstraints(constraints.ToArray());
		}

		void BuildConstraintsForCreditCardTextField(List<NSLayoutConstraint> constraints)
		{
			CreditCardTextField.TranslatesAutoresizingMaskIntoConstraints = false;
			constraints.Add(NSLayoutConstraint.Create(CreditCardTextField,
														NSLayoutAttribute.Top,
														NSLayoutRelation.Equal,
														   View,
														NSLayoutAttribute.Top,
														1,
														80));
			constraints.Add(NSLayoutConstraint.Create(CreditCardTextField,
													  NSLayoutAttribute.Left,
													  NSLayoutRelation.Equal,
													  View,
													  NSLayoutAttribute.Left,
													  1,
													  10));
			var width = View.Frame.Width - 20;
			constraints.Add(NSLayoutConstraint.Create(CreditCardTextField,
													  NSLayoutAttribute.Width,
													  NSLayoutRelation.Equal,
													  1,
													  width));

		}

		void BuildConstraintsForValidateButton(List<NSLayoutConstraint> constraints)
		{
			ValidateButton.TranslatesAutoresizingMaskIntoConstraints = false;

			constraints.Add(NSLayoutConstraint.Create(ValidateButton,
													  NSLayoutAttribute.Top,
													  NSLayoutRelation.Equal,
													  View,
													  NSLayoutAttribute.Top,
													  1,
													  115));

			var left = (View.Frame.Width - ValidateButton.Frame.Width)/2;
			constraints.Add(NSLayoutConstraint.Create(ValidateButton,
																  NSLayoutAttribute.Left,
																  NSLayoutRelation.Equal,
																  View,
																  NSLayoutAttribute.Left,
																  1,
																  left));


		}

		void BuildConstraintsForErrorMessage(List<NSLayoutConstraint> constraints)
		{
			ErrorMessagesTextField.TranslatesAutoresizingMaskIntoConstraints = false;
			constraints.Add(NSLayoutConstraint.Create(ErrorMessagesTextField,
													  NSLayoutAttribute.Top,
													  NSLayoutRelation.Equal,
													  View,
													  NSLayoutAttribute.Top,
													  1,
													  170));
			constraints.Add(NSLayoutConstraint.Create(ErrorMessagesTextField,
													  NSLayoutAttribute.Left,
													  NSLayoutRelation.Equal,
													  View,
													  NSLayoutAttribute.Left,
													  1,
													  10));

			var width = View.Frame.Width - 20;
			constraints.Add(NSLayoutConstraint.Create(ErrorMessagesTextField,
													  NSLayoutAttribute.Width,
										 			  NSLayoutRelation.Equal,
										 			  1,
										 			  width));
		}
	}
}

