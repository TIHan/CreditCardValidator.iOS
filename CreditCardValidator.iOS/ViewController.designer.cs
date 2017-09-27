// WARNING
//
// This file has been generated automatically by Xamarin Studio from the outlets and
// actions declared in your storyboard file.
// Manual changes to this file will not be maintained.
//
using Foundation;
using System;
using System.CodeDom.Compiler;
using UIKit;

namespace CreditCardValidator.iOS
{
	[Register ("ViewController")]
	partial class ViewController
	{
		[Outlet]
		[GeneratedCode ("iOS Designer", "1.0")]
		UITextField CreditCardTextField { get; set; }

		[Outlet]
		[GeneratedCode ("iOS Designer", "1.0")]
		UILabel ErrorMessagesTextField { get; set; }

		[Outlet]
		[GeneratedCode ("iOS Designer", "1.0")]
		UIButton ValidateButton { get; set; }

		[Action ("ValidateButton_TouchUpInside:")]
		[GeneratedCode ("iOS Designer", "1.0")]
		partial void ValidateButton_TouchUpInside (UIButton sender);

		void ReleaseDesignerOutlets ()
		{
			if (CreditCardTextField != null) {
				CreditCardTextField.Dispose ();
				CreditCardTextField = null;
			}
			if (ErrorMessagesTextField != null) {
				ErrorMessagesTextField.Dispose ();
				ErrorMessagesTextField = null;
			}
			if (ValidateButton != null) {
				ValidateButton.Dispose ();
				ValidateButton = null;
			}
		}
	}
}
